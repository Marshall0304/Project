origin = load('origin.txt');
pre = load('pre.txt');
plot(origin,'-b');
hold on;
plot(pre,'-r');
hold on;
count_1 = 0;
count_2 = 0;
count_5 = 0;
count_r = 0;
count_w = 0;
[count_x,count_y]=size(origin);  
for i=1:(count_x)
    error = (origin(i) - pre(i))/origin(i);
    if (error > -0.01 && error < 0.01)
        count_1 = count_1 + 1;
    end
    if (error > -0.02 && error < 0.02)
        count_2 = count_2 + 1;
    end
    if (error > -0.05 && error < 0.05)
        count_5 = count_5 + 1;
    end
    if (i > 7)
        if ((origin(i)-origin(i-7))>0&&(pre(i)-origin(i-7))>0 || (origin(i)-origin(i-7))<0&&(pre(i)-origin(i-7))<0)
            count_r = count_r + 1;
        else
            count_w = count_w + 1;
        end
    end
    
end  
fid=fopen('result.txt','w');
fprintf(fid,'0.01以内的误差准确率：%f\r\n0.02以内的误差准确率：%f\r\n0.05以内的误差准确率：%f\r\n涨跌准确率：%f\r\n',count_1/count_x,count_2/count_x,count_5/count_x,count_r/(count_r+count_w));  
fclose(fid) ;